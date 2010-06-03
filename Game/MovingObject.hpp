#pragma once

#include "Tree/Vec2.hpp"
#include "Tree/Rect.hpp"
#include "Tree/Timer.hpp"

class MovingObject {
public:
    MovingObject();
    virtual ~MovingObject() { }

    void SetPos( Tree::Vec2f p ) { pos = p; }
    void SetXPos( float x ) { pos.x = x; }
    void SetYPos( float y ) { pos.y = y; }
    Tree::Vec2f GetPos() const { return pos; }

    void SetVel( Tree::Vec2f v ) { vel = v; }
    void SetXVel( float xv ) { vel.x = xv; }
    void SetYVel( float yv ) { vel.y = yv; }
    Tree::Vec2f GetVel() const { return vel; }

    void MoveLeft();
    void MoveRight();
    void MoveUp();
    void MoveDown();

    void Stop();
    void Reset();

    bool IsMoving();

    bool WantsStop();

    bool WantsLeft();
    bool WantsRight();
    bool WantsUp();
    bool WantsDown();

    void FaceLeft();
    void FaceRight();
    void FaceUp();
    void FaceDown();

    bool FacesLeft();
    bool FacesRight();
    bool FacesUp();
    bool FacesDown();

    virtual float GetSpeed() = 0;

    void UpdateMovement( float dt );
protected:
    Tree::Vec2f pos;
    Tree::Vec2f vel;

    void ForceStop();
    Tree::Vec2f stop_pos;

    virtual void SetFaceLeft() { }
    virtual void SetFaceRight() { }
    virtual void SetFaceUp() { }
    virtual void SetFaceDown() { }

    Tree::Vec2f next_move;
    Tree::Vec2f face_dir;

    //this is the grid size
    const int size;
};
